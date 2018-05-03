#include <PrettyPrinter/DisasmData.h>
#include <PrettyPrinter/PrettyPrinter.h>
#include <boost/filesystem.hpp>
#include <boost/process.hpp>
#include <boost/program_options.hpp>
#include <iomanip>
#include <iostream>
#include "Logger.h"

int main(int argc, char** argv)
{
    boost::program_options::options_description desc("Allowed options");
    desc.add_options()("help", "Produce help message.");
    desc.add_options()("decode,c", boost::program_options::value<std::string>(),
                       "Decode a binary and call Souffle.");
    desc.add_options()("dir,d", boost::program_options::value<std::string>(),
                       "Set a datalog output directory to parse.  Automatically set (or "
                       "overwtitten) by the --decode optoin.");
    desc.add_options()("asm,a",
                       boost::program_options::value<std::string>()->default_value("out.asm"),
                       "The name of the assembly output file.");
    desc.add_options()("debug,D", boost::program_options::value<bool>()->default_value(false),
                       "Turn on debugging (will break assembly)");

    boost::program_options::variables_map vm;
    boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);

    if(vm.count("help") != 0 || argc == 1)
    {
        std::cout << desc << "\n";
        return 1;
    }

    boost::program_options::notify(vm);

    boost::filesystem::path disasmPath;

    // "dir" can be overwritten by "decode".
    if(vm.count("dir") != 0)
    {
        disasmPath = vm["dir"].as<std::string>();
    }

    if(vm.count("decode") != 0)
    {
        boost::filesystem::path exe = vm["decode"].as<std::string>();

        if(boost::filesystem::is_regular_file(exe) == true)
        {
            const auto exePath = exe.parent_path();
            disasmPath = exePath / "dl_files/";

            if(boost::filesystem::is_directory(disasmPath) == false)
            {
                if(boost::filesystem::create_directory(disasmPath) == false)
                {
                    LOG_ERROR << "Could not create directory " << disasmPath << "." << std::endl;
                    return EXIT_FAILURE;
                }
                else
                {
                    LOG_INFO << "Created directory " << disasmPath << std::endl;
                }
            }

            // Step 1: Call datalog_decoder.
            {
                std::stringstream cmd;
                cmd << "datalog_decoder --file " << exe << " --dir " << disasmPath
                    << " --sect .plt.got --sect .fini --sect .init --sect .plt --sect .text "
                       "--data_sect "
                       ".data --data_sect .rodata --data_sect .fini_array --data_sect .init_array "
                       "--data_sect .data.rel.ro --data_sect .got.plt --data_sect .got";

                LOG_DEBUG << cmd.str() << std::endl;
                LOG_DEBUG << std::endl;

                try
                {
                    const auto datalogDecoderResult = boost::process::system(cmd.str());

                    if(datalogDecoderResult == 0)
                    {
                        LOG_INFO << "Datalog Decoder Success." << std::endl;
                    }
                    else
                    {
                        LOG_ERROR << "Datalog Decoder Failure." << std::endl
                                  << "\tCMD: \"" << cmd.str() << "\"" << std::endl;
                        return EXIT_FAILURE;
                    }
                }
                catch(const std::exception& e)
                {
                    LOG_ERROR << e.what() << std::endl;
                    LOG_ERROR << "Make sure that \"datalog_decoder\" is in your PATH." << std::endl;
                    return EXIT_FAILURE;
                }
            }

            // Step 2: Call souffle
            {
                std::stringstream cmd;
                cmd << "souffle_disasm  -F " << disasmPath << " -D " << disasmPath;

                LOG_DEBUG << cmd.str() << std::endl;
                LOG_DEBUG << std::endl;

                try
                {
                    const auto datalogDecoderResult = boost::process::system(cmd.str());

                    if(datalogDecoderResult == 0)
                    {
                        LOG_INFO << "Souffle Success." << std::endl;
                    }
                    else
                    {
                        LOG_ERROR << "Souffle Failure." << std::endl
                                  << "\tCMD: \"" << cmd.str() << "\"" << std::endl;
                        return EXIT_FAILURE;
                    }
                }
                catch(const std::exception& e)
                {
                    LOG_ERROR << e.what() << std::endl;
                    LOG_ERROR << "Make sure that \"souffle_disasm\" is in your PATH." << std::endl;
                    return EXIT_FAILURE;
                }
            }
        }
        else
        {
            LOG_ERROR << "The parameter " << exe << " is not a file." << std::endl;
            return EXIT_FAILURE;
        }
    }

    if(boost::filesystem::is_directory(disasmPath) == true)
    {
        DisasmData disasm;

        LOG_INFO << std::setw(24) << std::left << "Reading Directory: " << disasmPath << std::endl;

        disasm.parseDirectory(disasmPath.string());

        // Perform the Pretty Printing step.
        PrettyPrinter pp;
        pp.setDebug(vm["debug"].as<bool>());
        const auto assembly = pp.prettyPrint(&disasm);

        // Do we write it to a file?
        if(vm.count("asm") != 0)
        {
            const auto asmPath = boost::filesystem::path(vm["asm"].as<std::string>());
            std::ofstream ofs;
            ofs.open(asmPath.string());

            if(ofs.is_open() == true)
            {
                ofs << assembly;
                ofs.close();
                LOG_INFO << "Assembly written to: " << asmPath << "\n";
            }
            else
            {
                LOG_ERROR << "Could not output assembly output file: " << asmPath << "\n";
            }
        }
        else
        {
            std::cout << assembly << std::endl;
        }
    }
    else
    {
        LOG_ERROR << "Disassembly directory not found: \"" << disasmPath << "\".";
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
